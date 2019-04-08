import React       from 'react';
import styles      from './Attribute.module.css';

interface Props {
    icon: string;
    name: string;
    value: string;
}

const Attribute = (props: Props) =>
    <div className={styles.Root}>
        <span>
            <i className={`icofont-${props.icon}`} />
            {props.name}
        </span>

        <span>
            {props.value}
        </span>
    </div>;

export default Attribute;
