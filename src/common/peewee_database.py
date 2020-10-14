import logging
import time
import datetime

from peewee import *

from playhouse.reflection import generate_models, print_model, print_table_sql
from playhouse.migrate import *

logger = logging.getLogger(__name__)

import settings

CONNECTION_FORMAT = "postgresql+psycopg2://{user}:{password}@{host}:{port}/{database}"
PARAMETER_DICT = {
    "host": settings.PG_HOST,
    "port": 5432,
    "user": settings.PG_USER,
    "password": settings.PG_PASSWORD,
    "database": "doktor_test",
}


def _get_engine(**kwargs) -> PostgresqlDatabase:
    engine = PostgresqlDatabase(**kwargs)

    try:
        assert engine.connect()

    except Exception as exception:
        logger.error(f"Failed to get connection: {exception}")

        raise exception

    return engine


engine = _get_engine(**PARAMETER_DICT)


class BaseModel(Model):
    class Meta:
        database = engine
        schema = "base"


class Conversation(BaseModel):
    class Meta:
        table_name = "conversation"

    StateList = ["created", "opened", "closed", "archived"]
    id = UUIDField(primary_key=True, default=lambda: str(uuid.uuid4()))
    patient_id = CharField(null=False, index=True)
    patient_id_external = CharField(null=True)
    patient_gender = CharField(null=True)
    patient_created = DateTimeField(null=True)
    patient_date_of_birth = CharField(null=True)
    conversation_opened = DateTimeField(null=True)
    conversation_closed = DateTimeField(null=True)
    created_by_staff = BooleanField(null=False)
    queue_start = DateTimeField(null=True)
    first_assigned = DateTimeField(null=True)
    first_category = CharField(null=True)
    category = CharField(null=False)
    staff_id_1 = CharField(null=True)
    staff_1_role = CharField(null=True)
    staff_id_2 = CharField(null=True)
    staff_2_role = CharField(null=True)
    staff_id_3 = CharField(null=True)
    staff_3_role = CharField(null=True)
    staff_id_4 = CharField(null=True)
    staff_4_role = CharField(null=True)
    staff_id_5 = CharField(null=True)
    staff_5_role = CharField(null=True)
    staff_id_6 = CharField(null=True)
    staff_6_role = CharField(null=True)
    staff_views = IntegerField(null=True)
    call_in_conversation = BooleanField(null=False)
    sip_in_conversation = BooleanField(null=False)
    number_of_messages = IntegerField(null=False)
    rating_1 = CharField(null=True)
    rating_2 = CharField(null=True)
    mins_to_first_assign = IntegerField(null=True)
    emergency_queue = BooleanField(null=True)
    number_of_staff_messages = IntegerField(null=True)
    number_of_client_messages = IntegerField(null=True)
    number_of_bot_messages = IntegerField(null=True)
    call_time_in_seconds = IntegerField(null=True)
    sip_call_time_in_seconds = IntegerField(null=True)
    patient_cancelled = BooleanField(null=True)
    close_reason = CharField(null=True)
    language = CharField(null=True)
    region = CharField(null=True)
    highest_level_of_care = CharField(null=True)
    profile_id = UUIDField(null=True)
    referrer_id = CharField(null=True)
    charged_county = CharField(null=True)
    payment = CharField(
        null=True,
        choices=(
            ("payment_accepted", "payment_accepted"),
            ("card_exempt", "card_exempt"),
            ("age_exempt", "age_exempt"),
        ),
    )
    payment_ref = CharField(null=True)


migrator = PostgresqlMigrator(engine)

models = generate_models(engine)


migrator = SchemaMigrator.from_database(engine)
